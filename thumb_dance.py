from manim import *
import numpy as np

# VERTICAL (Instagram)
config.pixel_height = 1920
config.pixel_width = 1080
config.frame_height = 16
config.frame_width = 9


class ThumbDance(Scene):
    def construct(self):
        self.cena1_tutorial_setor_inclinado()
        self.cena2_funcao_interativa_centro()
        self.cena3_integral_impacto()
        self.cena4_formas_flip()
        self.cena5_palavras_impacto()

    # ---------------------------
    # Visual do dedo
    # ---------------------------
    def make_thumb(self):
        thumb = RoundedRectangle(
            width=1.1, height=2.1, corner_radius=0.35
        ).set_fill(WHITE, opacity=1).set_stroke(width=0)

        nail = RoundedRectangle(
            width=0.75, height=0.55, corner_radius=0.18
        ).set_fill(GRAY_B, opacity=1).set_stroke(width=0)

        nail.move_to(thumb.get_top() + DOWN * 0.55)
        return VGroup(thumb, nail)

    # ---------------------------
    # Cena 1
    # ---------------------------
    def cena1_tutorial_setor_inclinado(self):
        # Quina inferior direita
        CENTER = np.array([4.5, -8.0, 0.0])

        # Raio do movimento
        R = 7.2

        # Setor inclinado (edite aqui se quiser)
        theta_start = 0.80 * np.pi
        theta_end   = 0.55 * np.pi

        # Tracker do ângulo
        t = ValueTracker(theta_start)

        def point_at(theta):
            return CENTER + R * np.array([np.cos(theta), np.sin(theta), 0.0])

        # Dedo radial
        thumb_base = self.make_thumb().scale(0.9)
        radial_offset = -np.pi / 2  # dedo base aponta "pra cima"

        thumb = always_redraw(
            lambda: thumb_base.copy()
                .rotate(t.get_value() + radial_offset)
                .move_to(point_at(t.get_value()))
        )

        # Comprimento variável da seta
        # >>> EDITE AQUI <<<
        L_MIN = 0.35
        L_MAX = 1.80
        BACK  = 0.20  # recuo atrás do dedo

        # Sinal do movimento na ida: +1 se theta cresce, -1 se theta decresce
        base_sign = 1.0 if (theta_end - theta_start) >= 0 else -1.0

        # Fase (progresso) ao longo do arco (0 no início, 1 no fim)
        def progress(theta, a, b):
            if abs(b - a) < 1e-9:
                return 0.0
            f = (theta - a) / (b - a)
            return float(np.clip(f, 0.0, 1.0))

        # Envelope 0->1->0 baseado no progresso (máximo no meio)
        def envelope(f):
            return 4 * f * (1 - f)

        # A seta deve apontar no sentido do movimento:
        # vetor tangente = (-sin(theta), cos(theta)) * sign(dtheta/dt)
        # Na ida: sign(dtheta/dt) = base_sign
        # Na volta: sign(dtheta/dt) = -base_sign
        motion_sign = ValueTracker(base_sign)  # será +base_sign na ida e -base_sign na volta

        arrow = always_redraw(
            lambda: self._tangent_arrow_variable_length(
                point=point_at(t.get_value()),
                theta=t.get_value(),
                motion_sgn=motion_sign.get_value(),
                length=L_MIN + (L_MAX - L_MIN) * envelope(progress(t.get_value(), theta_start, theta_end)),
                back=BACK
            )
        )

        # Arco guia opcional (bem sutil)
        arc = Arc(
            radius=R,
            start_angle=theta_start,
            angle=(theta_end - theta_start),
            arc_center=CENTER
        ).set_stroke(RED, width=6, opacity=0.25)

        text = Text("Mova seu dedo no ritmo!", font_size=48).to_edge(UP, buff=2)

        self.add(arrow, thumb, text)

        # Tempos (você vai ajustar no editor)
        ida = 1.0
        volta = 1.0

        # IDA
        motion_sign.set_value(base_sign)
        self.play(t.animate.set_value(theta_end), run_time=ida, rate_func=smooth)

        # VOLTA
        motion_sign.set_value(-base_sign)
        self.play(t.animate.set_value(theta_start), run_time=volta, rate_func=smooth)
        self.wait()

        self.play(FadeOut(thumb, arrow, text))

        

    def _tangent_arrow_variable_length(self, point, theta, motion_sgn, length=1.5, back=0.2):
        # tangente unitária
        tan = np.array([-np.sin(theta), np.cos(theta), 0.0])
        n = np.linalg.norm(tan)
        if n < 1e-9:
            tan = RIGHT
        else:
            tan = tan / n

        # aplica sentido do movimento
        tan = motion_sgn * tan

        start = point - back * tan
        end = start + length * tan

        a = Arrow(start, end, buff=0, tip_length=0.25)
        a.set_stroke(RED, width=10)
        return a

    # ---------------------------
    # Cena 2: função construída pelo dedo
    # ---------------------------
    def cena2_funcao_interativa_centro(self):
        # Progresso do "mundo": o plano anda para a esquerda
        cam_t = ValueTracker(0.0)
        draw_t = ValueTracker(0.0)
        x_scale = 1.2
        world_start = -4.2
        world_end = 8.0

        # Niveis da sequencia:
        # sobe - desce - sobe - desce - sobe muito
        y_0 = -1.10
        y_1 = 1.00
        y_2 = -0.95
        y_3 = 0.85
        y_4 = -0.80
        y_5 = 3.80

        x_up1_a, x_up1_b = -3.6, -2.8
        x_dn1_a, x_dn1_b = -2.0, -1.2
        x_up2_a, x_up2_b = -0.3, 0.5
        x_dn2_a, x_dn2_b = 1.3, 2.1
        x_up3_a, x_up3_b = 2.8, 4.0

        # Ease quadratico (lento no inicio/fim) para a queda
        def ease_in_out_quad(u):
            if u < 0.5:
                return 2 * u * u
            return 1 - ((-2 * u + 2) ** 2) / 2

        def f_base(wx):
            if wx <= x_up1_a:
                return y_0
            if wx <= x_up1_b:
                u = (wx - x_up1_a) / (x_up1_b - x_up1_a)
                return y_0 + (y_1 - y_0) * ease_in_out_quad(u)
            if wx <= x_dn1_a:
                return y_1
            if wx <= x_dn1_b:
                u = (wx - x_dn1_a) / (x_dn1_b - x_dn1_a)
                return y_1 + (y_2 - y_1) * ease_in_out_quad(u)
            if wx <= x_up2_a:
                return y_2
            if wx <= x_up2_b:
                u = (wx - x_up2_a) / (x_up2_b - x_up2_a)
                return y_2 + (y_3 - y_2) * ease_in_out_quad(u)
            if wx <= x_dn2_a:
                return y_3
            if wx <= x_dn2_b:
                u = (wx - x_dn2_a) / (x_dn2_b - x_dn2_a)
                return y_3 + (y_4 - y_3) * ease_in_out_quad(u)
            if wx <= x_up3_a:
                return y_4
            if wx <= x_up3_b:
                u = (wx - x_up3_a) / (x_up3_b - x_up3_a)
                return y_4 + (y_5 - y_4) * ease_in_out_quad(u)
            return y_5

        # Converte coordenada do mundo para tela usando plano movel
        def to_screen(wx, wy):
            x_now = world_start + (world_end - world_start) * cam_t.get_value()
            y_now = f_base(x_now)
            sx = (wx - x_now) * x_scale
            sy = wy - y_now
            return np.array([sx, sy, 0.0])

        # Eixo x com valores, pertencente ao plano que se move
        def moving_x_axis():
            x_now = world_start + (world_end - world_start) * cam_t.get_value()
            y_now = f_base(x_now)
            axis = Line(LEFT * 4.6, RIGHT * 4.6).set_stroke(GRAY_B, 4).shift(DOWN * y_now)
            marks = VGroup()

            n_start = int(np.floor(x_now - 4.6 / x_scale)) - 1
            n_end = int(np.ceil(x_now + 4.6 / x_scale)) + 1
            for n in range(n_start, n_end + 1):
                sx = (n - x_now) * x_scale
                tick = Line([sx, -0.12 - y_now, 0], [sx, 0.12 - y_now, 0]).set_stroke(GRAY_B, 3)
                label = MathTex(str(n), font_size=30, color=GRAY_A).move_to([sx, -0.45 - y_now, 0])
                marks.add(tick, label)
            return VGroup(axis, marks)

        x_axis_group = always_redraw(moving_x_axis)

        # Curva sendo construida pelo ponto fixo (amostras acumuladas do passado)
        def built_curve():
            x_now = world_start + (world_end - world_start) * draw_t.get_value()
            sample_x = np.linspace(world_start, x_now, 320)
            pts = [to_screen(wx, f_base(wx)) for wx in sample_x]
            curve = VMobject(color=RED)
            if len(pts) >= 2:
                curve.set_points_as_corners(pts)
            curve.set_stroke(width=8)
            return curve

        graph = always_redraw(built_curve)

        # Ponto marcador: segue o ponto atual do desenho
        def marker_pos():
            x_now = world_start + (world_end - world_start) * draw_t.get_value()
            return to_screen(x_now, f_base(x_now))

        fixed_ring = always_redraw(
            lambda: Circle(radius=0.18, color=YELLOW, stroke_width=3).move_to(marker_pos())
        )
        fixed_dot = always_redraw(
            lambda: Dot(marker_pos(), color=YELLOW, radius=0.09)
        )

        self.add(x_axis_group, graph, fixed_ring, fixed_dot)
        # Camera/plano e desenho andam juntos na cena toda
        self.play(
            cam_t.animate.set_value(1.0),
            draw_t.animate.set_value(1.0),
            run_time=5.4,
            rate_func=linear,
        )
        self.remove(x_axis_group, graph, fixed_ring, fixed_dot)

    # ---------------------------
    # Cena 3: impacto da integral
    # ---------------------------
    def cena3_integral_impacto(self):
        # Expressao inicial no topo-centro
        expr_ini = MathTex("x^3", font_size=110).move_to(ORIGIN + 2 * UP)
        expr_final = MathTex(r"\frac{x^4}{4}", "+c", font_size=98).move_to(expr_ini.get_center())

        # Ponto de entrada perto da posicao inicial do dedo da cena 1
        alvo_integral = np.array([-1.3, -3.7, 0.0])
        integral = MathTex(r"\int\,dx", font_size=80)
        integral.move_to(alvo_integral + LEFT * 5.2)
        menos_c = MathTex("-c", font_size=98)
        menos_c.move_to(alvo_integral + LEFT * 5.2)

        self.play(FadeIn(expr_ini), run_time=0.25)

        # Entrada rapida pela esquerda
        self.play(integral.animate.move_to(alvo_integral), run_time=0.22, rate_func=rush_into)
        self.wait(0.5)

        # Arremesso contra o x^3
        self.play(
            integral.animate.move_to(expr_ini.get_center() + 0.12 * LEFT),
            run_time=0.35,
            rate_func=rush_from,
        )

        # Impacto: os dois objetos viram pontos
        impact_point = expr_ini.get_center()
        dot_expr = Dot(impact_point, color=WHITE, radius=0.07)
        dot_int = Dot(impact_point, color=WHITE, radius=0.07)
        self.add(menos_c)
        self.play(
            Transform(expr_ini, dot_expr),
            Transform(integral, dot_int),
            menos_c.animate(rate_func=rush_into).move_to(alvo_integral),
            Flash(impact_point, color=YELLOW, line_length=0.25, flash_radius=0.8),
            run_time=0.2,
            rate_func=smooth,
        )

        # Pontos viram resultado final (+c incluido)
        self.play(
            FadeOut(integral, scale=0.5),
            ReplacementTransform(expr_ini, expr_final),
            run_time=0.2,
        )

        # Entra -c e anula o +c
        self.wait(0.5)
        self.play(menos_c.animate.move_to(expr_final[1].get_center()), run_time=0.35, rate_func=rush_from)
        proj_primeiro = MathTex(r"\times 4", font_size=80, color=WHITE).move_to(alvo_integral + RIGHT * 5.2)
        self.add(proj_primeiro)
        self.play(
            expr_final[1].animate.set_color(RED),
            menos_c.animate.set_color(RED),
            proj_primeiro.animate(rate_func=rush_into).move_to(alvo_integral),
            Flash(expr_final[1].get_center(), color=RED, line_length=0.22, flash_radius=0.55),
            run_time=0.18,
        )
        self.play(FadeOut(expr_final[1], scale=0.7), FadeOut(menos_c, scale=0.7), run_time=0.18)
        self.play(expr_final[0].animate.move_to(ORIGIN + 2 * UP), run_time=0.2)
        expr_atual = expr_final[0]
        self.wait(0.35)

        # Helper para criar o objeto "projetil" de cada etapa
        def make_proj(proj_tex, side):
            proj = MathTex(proj_tex, font_size=80, color=WHITE)
            offset = LEFT * 5.2 if side == "left" else RIGHT * 5.2
            proj.move_to(alvo_integral + offset)
            return proj

        # Helper: arremessa no alvo e transforma a expressao.
        # Se next_proj vier, ele ja entra durante a colisao atual.
        def impacto_transforma(
            expr_obj,
            proj,
            resultado_tex,
            font_size=96,
            proj_prepared=False,
            next_proj=None,
        ):
            self.add(proj)
            if not proj_prepared:
                self.play(proj.animate.move_to(alvo_integral), run_time=0.22, rate_func=rush_into)

            self.wait(0.5)
            self.play(
                proj.animate.move_to(expr_obj.get_center() + 0.12 * LEFT),
                run_time=0.35,
                rate_func=rush_from,
            )

            p = expr_obj.get_center()
            dot_expr = Dot(p, color=WHITE, radius=0.07)
            dot_proj = Dot(p, color=WHITE, radius=0.07)

            colisoes = [
                Transform(expr_obj, dot_expr),
                Transform(proj, dot_proj),
                Flash(p, color=YELLOW, line_length=0.25, flash_radius=0.8),
            ]
            if next_proj is not None:
                self.add(next_proj)
                colisoes.append(next_proj.animate(rate_func=rush_into).move_to(alvo_integral))

            self.play(*colisoes, run_time=0.22, rate_func=smooth)

            novo = MathTex(resultado_tex, font_size=font_size).move_to(p)
            self.play(FadeOut(proj, scale=0.5), ReplacementTransform(expr_obj, novo), run_time=0.2)
            return novo

        # Sequencia: *4 -> soma termos -> fatora -> multiplica por (x^2)^(-1)
        steps = [
            (r"\times 4", r"x^4", "right", 96),
            (r"+2x^3+x^2", r"x^4+2x^3+x^2", "left", 92),
            (r"()", r"x^2(x^2+2x+1)", "right", 90),
            (r"\left(x^2\right)^{-1}", r"x^2+2x+1", "left", 92),
        ]

        proj_atual = proj_primeiro
        proj_prepared = True
        for i, (_, resultado_tex, _, fs) in enumerate(steps):
            next_proj = None
            if i + 1 < len(steps):
                next_proj = make_proj(steps[i + 1][0], steps[i + 1][2])

            expr_atual = impacto_transforma(
                expr_atual,
                proj_atual,
                resultado_tex,
                font_size=fs,
                proj_prepared=proj_prepared,
                next_proj=next_proj,
            )
            proj_atual = next_proj
            proj_prepared = True
        self.wait(0.4)

    # ---------------------------
    # Cena 4: formas com rotacao/flip
    # ---------------------------
    def cena4_formas_flip(self):
        pos_base = ORIGIN + 2 * DOWN

        # Formas principais
        circulo = Circle(radius=1.1, color=BLUE_C).set_fill(BLUE_E, opacity=0.9).move_to(pos_base)
        quadrado = Square(side_length=2.2, color=GREEN_C).set_fill(GREEN_E, opacity=0.9).move_to(pos_base)
        triangulo = RegularPolygon(n=3, radius=1.35, color=ORANGE).set_fill(RED_E, opacity=0.9).move_to(pos_base)
        pentagono = RegularPolygon(n=5, radius=1.3, color=PURPLE_B).set_fill(PURPLE_E, opacity=0.9).move_to(pos_base)
        estrela = Star(n=5, outer_radius=1.35, color=YELLOW).set_fill(GOLD_E, opacity=0.9).move_to(pos_base)

        # Cria uma versao "achatada" no eixo diagonal desejado
        def achatar_no_eixo(mob, direcao, fator=0.02):
            m = mob.copy()
            centro = m.get_center()
            ang = angle_of_vector(direcao)
            m.rotate(-ang, about_point=centro)
            m.stretch(fator, 0, about_point=centro)
            m.rotate(ang, about_point=centro)
            return m

        # Flip 2D sem deslocar o objeto: so troca de face no mesmo ponto
        def flip_para(atual, proximo, direcao, tempo=0.9):
            ease_in = lambda t: t * t
            ease_out = lambda t: 1 - (1 - t) * (1 - t)

            atual_flat = achatar_no_eixo(atual, direcao)
            proximo.move_to(pos_base)
            proximo_flat = achatar_no_eixo(proximo, direcao)

            self.play(Transform(atual, atual_flat), run_time=tempo * 0.45, rate_func=ease_in)
            self.play(
                Flash(pos_base, color=YELLOW, line_length=0.22, flash_radius=0.75),
                run_time=0.16,
            )
            self.remove(atual)
            self.add(proximo_flat)
            self.play(Transform(proximo_flat, proximo), run_time=tempo * 0.55, rate_func=ease_out)
            self.play(proximo.animate.scale(1.05), run_time=0.08, rate_func=smooth)
            self.play(proximo.animate.scale(1 / 1.05), run_time=0.08, rate_func=smooth)
            self.remove(proximo_flat)
            self.add(proximo)

        self.play(FadeIn(circulo, scale=0.9), run_time=0.3)
        self.wait(0.2)

        # Circulo gira para diagonal superior direita e vira quadrado
        flip_para(circulo, quadrado, UR, tempo=0.85)
        self.wait(0.2)

        # Quadrado gira para a outra diagonal e vira triangulo
        flip_para(quadrado, triangulo, DL, tempo=0.9)
        self.wait(0.2)

        # Triangulo gira para diagonal superior direita e vira pentagono
        flip_para(triangulo, pentagono, DL, tempo=0.9)
        self.wait(0.2)

        # Pentagono gira para diagonal superior direita e vira estrela
        flip_para(pentagono, estrela, UR, tempo=0.95)
        self.wait(0.35)

    # ---------------------------
    # Cena 5: palavras com arrasto e impacto no centro
    # ---------------------------
    def cena5_palavras_impacto(self):
        palavras = [
            "Cálculo",
            "Álgebra Linear",
            "Análise Real",
            "Geometria",
            "Espaços Métricos",
            "Topologia",
            "Análise Complexa",
            "Medida",
        ]

        entrada_base = np.array([-5.2, 8.3, 0.0])
        impacto = ORIGIN

        for i, texto in enumerate(palavras):
            palavra = Text(texto, font_size=76, color=WHITE).move_to(entrada_base + 0.24 * i * DOWN)

            # Rastro de arrasto
            trilha = TracedPath(
                palavra.get_center,
                stroke_color=YELLOW,
                stroke_opacity=[0.0, 0.7, 0.0],
                stroke_width=5,
                dissipating_time=0.28,
            )

            self.add(trilha, palavra)

            # Entrada agressiva para o centro
            self.play(
                palavra.animate.move_to(impacto),
                run_time=0.42,
                rate_func=rush_from,
            )

            # Arremesso alternado: cima-direita, baixo-esquerda, ...
            # Impacto e empurrao acontecem juntos, sem atraso.
            direcao_saida = normalize((RIGHT + UP) if i % 2 == 0 else (LEFT + DOWN))
            self.play(
                Flash(impacto, color=YELLOW, line_length=0.35, flash_radius=1.0),
                palavra.animate.shift(4.8 * direcao_saida).rotate(8 * DEGREES),
                run_time=0.26,
                rate_func=rush_from,
            )
            self.play(FadeOut(palavra, scale=0.92), run_time=0.08)
            self.remove(trilha)

        self.wait(0.3)

import { Link } from "react-router";
import { Button } from "../Button";

type FeedbackButtonProps = { className?: string };

export function FeedbackButton({ className }: FeedbackButtonProps) {
  return (
    <Link
      to="https://forms.gle/x4e2EbqpgMYXzPbC9"
      target="_blank"
      className="lg:ml-auto"
    >
      <Button type="tertiary" title="Give feedback" className={className} />
    </Link>
  );
}
